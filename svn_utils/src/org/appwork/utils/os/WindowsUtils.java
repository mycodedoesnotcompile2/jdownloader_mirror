/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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

import static com.sun.jna.platform.win32.WinNT.DACL_SECURITY_INFORMATION;
import static com.sun.jna.platform.win32.WinNT.FILE_ALL_ACCESS;
import static com.sun.jna.platform.win32.WinNT.FILE_GENERIC_EXECUTE;
import static com.sun.jna.platform.win32.WinNT.FILE_GENERIC_READ;
import static com.sun.jna.platform.win32.WinNT.FILE_GENERIC_WRITE;
import static com.sun.jna.platform.win32.WinNT.GROUP_SECURITY_INFORMATION;
import static com.sun.jna.platform.win32.WinNT.OWNER_SECURITY_INFORMATION;
import static com.sun.jna.platform.win32.WinNT.STANDARD_RIGHTS_READ;
import static com.sun.jna.platform.win32.WinNT.TOKEN_DUPLICATE;
import static com.sun.jna.platform.win32.WinNT.TOKEN_IMPERSONATE;
import static com.sun.jna.platform.win32.WinNT.TOKEN_QUERY;
import static com.sun.jna.platform.win32.WinUser.SW_HIDE;
import static com.sun.jna.platform.win32.WinUser.SW_SHOW;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.jna.windows.Rm;
import org.appwork.jna.windows.RmProcessInfo;
import org.appwork.jna.windows.interfaces.Advapi32Ext;
import org.appwork.jna.windows.interfaces.ByHandleFileInformation;
import org.appwork.jna.windows.interfaces.ExplicitAccess;
import org.appwork.jna.windows.interfaces.Trustee;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.StorableDoc;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.IO.BOM;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.Joiner;
import org.appwork.utils.StringUtils;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.StringArray;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.AccCtrl;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Advapi32Util.Account;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.Shell32;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.BOOLByReference;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.DWORDByReference;
import com.sun.jna.platform.win32.WinDef.INT_PTR;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.ACL;
import com.sun.jna.platform.win32.WinNT.GENERIC_MAPPING;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinNT.PACLByReference;
import com.sun.jna.platform.win32.WinNT.PRIVILEGE_SET;
import com.sun.jna.platform.win32.WinNT.PSID;
import com.sun.jna.platform.win32.WinNT.SECURITY_DESCRIPTOR_RELATIVE;
import com.sun.jna.platform.win32.WinNT.SECURITY_IMPERSONATION_LEVEL;
import com.sun.jna.platform.win32.WinNT.SID_NAME_USE;
import com.sun.jna.platform.win32.WinNT.TOKEN_ELEVATION;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Utility class for Windows-specific operations and permissions management. This class provides methods to handle Windows-specific
 * functionality like: - Process elevation and management - File permissions checking - User account and SID management - Security
 * descriptor operations
 *
 * @author Thomas
 * @date 14.10.2018
 */
public class WindowsUtils {
    public static enum AccessPermission {
        // File/Directory specific access rights
        @StorableDoc("For a file object, the right to read the corresponding file data. For a directory object, the right to read the corresponding directory data.")
        FILE_READ_DATA(WinNT.FILE_READ_DATA),
        @StorableDoc("For a directory object, the right to list the contents of the directory.")
        FILE_LIST_DIRECTORY(WinNT.FILE_LIST_DIRECTORY),
        @StorableDoc("For a file object, the right to write data to the file. For a directory object, the right to create a file in the directory.")
        FILE_WRITE_DATA(WinNT.FILE_WRITE_DATA),
        @StorableDoc("For a directory object, the right to create a file in the directory.")
        FILE_ADD_FILE(WinNT.FILE_ADD_FILE),
        @StorableDoc("For a file object, the right to append data to the file. For a directory object, the right to create a subdirectory.")
        FILE_APPEND_DATA(WinNT.FILE_APPEND_DATA),
        @StorableDoc("For a directory object, the right to create a subdirectory.")
        FILE_ADD_SUBDIRECTORY(WinNT.FILE_ADD_SUBDIRECTORY),
        @StorableDoc("For a named pipe, the right to create a pipe instance.")
        FILE_CREATE_PIPE_INSTANCE(WinNT.FILE_CREATE_PIPE_INSTANCE),
        @StorableDoc("The right to read extended attributes.")
        FILE_READ_EA(WinNT.FILE_READ_EA),
        @StorableDoc("The right to write extended attributes.")
        FILE_WRITE_EA(WinNT.FILE_WRITE_EA),
        @StorableDoc("The right to execute a file.")
        FILE_EXECUTE(WinNT.FILE_EXECUTE),
        @StorableDoc("For a directory object, the right to traverse the directory.")
        FILE_TRAVERSE(WinNT.FILE_TRAVERSE),
        @StorableDoc("For a directory object, the right to delete entries within the directory.")
        FILE_DELETE_CHILD(WinNT.FILE_DELETE_CHILD),
        @StorableDoc("The right to delete the object.")
        DELETE(WinNT.DELETE),
        @StorableDoc("The right to read file attributes.")
        FILE_READ_ATTRIBUTES(WinNT.FILE_READ_ATTRIBUTES),
        @StorableDoc("The right to write file attributes.")
        FILE_WRITE_ATTRIBUTES(WinNT.FILE_WRITE_ATTRIBUTES),
        @StorableDoc("All possible access rights for a file.")
        FILE_ALL_ACCESS(WinNT.FILE_ALL_ACCESS),
        @StorableDoc("Generic read access.")
        FILE_GENERIC_READ(WinNT.FILE_GENERIC_READ),
        @StorableDoc("Generic write access.")
        FILE_GENERIC_WRITE(WinNT.FILE_GENERIC_WRITE),
        @StorableDoc("Generic execute access.")
        FILE_GENERIC_EXECUTE(WinNT.FILE_GENERIC_EXECUTE),
        // Security descriptor access rights - not for CreateFile usage
        @StorableDoc("The right to read the security descriptor and ownership.")
        READ_CONTROL(WinNT.READ_CONTROL),
        @StorableDoc("The right to modify the discretionary access control list (DACL) in the object's security descriptor.")
        WRITE_DAC(WinNT.WRITE_DAC),
        @StorableDoc("The right to change the owner in the object's security descriptor.")
        WRITE_OWNER(WinNT.WRITE_OWNER),
        @StorableDoc("The right to use the object for synchronization.")
        SYNCHRONIZE(WinNT.SYNCHRONIZE),
        @StorableDoc("Access system security.")
        ACCESS_SYSTEM_SECURITY(WinNT.ACCESS_SYSTEM_SECURITY);

        public final int mask;

        private AccessPermission(int mask) {
            this.mask = mask;
        }
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

    // stay
    /**
     * @deprecated user WindowsUtils.SID enum instead we keep this field, because we would have to rebuild all Connect Service
     *             SelfTestPackages if we remove it.
     */
    public static final String SID_LOCAL_SYSTEM = "S-1-5-18";

    // public static final String SID_USER = "S-1-5-32-545";
    // public static final String SID_EVERYBODYY = "S-1-1-0";
    // public static final String SID_ADMINISTRATOR = "S-1-5-32-544";
    /**
     * Gets the current user's account information. This method retrieves the account details of the currently logged-in user.
     *
     * @return Account object containing user information
     * @throws Win32Exception
     *             if the operation fails
     */
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
     * Gets all accounts associated with the current user, including group memberships.
     *
     * @return Set of Account objects representing the user and their group memberships
     * @throws Win32Exception
     *             if the operation fails
     */
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
     * Returns the current user's Security Identifier (SID).
     *
     * @return String representation of the user's SID
     * @throws Exception
     *             if the operation fails
     */
    public static String getCurrentUserSID() throws Exception {
        return getCurrentUserAccount().sidString;
    }

    /**
     * Checks which file permissions are missing for a given directory path.
     *
     * @param dirPath
     *            The directory path to check
     * @param permissions
     *            The permissions to verify
     * @return Set of missing AccessPermission values
     */
    public static Set<AccessPermission> getMissingPermissionsViaHandle(File dirPath, AccessPermission... permissions) {
        int access = 0;
        for (AccessPermission p : permissions) {
            access |= p.mask;
        }
        HashSet<AccessPermission> ret = new HashSet<AccessPermission>();
        HANDLE h = Kernel32.INSTANCE.CreateFile(dirPath.getAbsolutePath(), access, WinNT.FILE_SHARE_READ | WinNT.FILE_SHARE_WRITE | WinNT.FILE_SHARE_DELETE, null, WinNT.OPEN_EXISTING, WinNT.FILE_FLAG_BACKUP_SEMANTICS, null);
        boolean hasAccess = !WinBase.INVALID_HANDLE_VALUE.equals(h);
        if (hasAccess) {
            Kernel32.INSTANCE.CloseHandle(h);
            return ret;
        }
        for (AccessPermission p : permissions) {
            if (!checkFileAccessViaHandle(dirPath, p)) {
                ret.add(p);
            }
        }
        return ret;
    }

    /**
     * Gets the principal names associated with the current user.
     *
     * @return Set of principal names in format "domain\\username"
     */
    public static Set<String> getMyPrincipalNames() {
        Set<String> myPrincipals = new HashSet<String>();
        for (Account a : getCurrentUsersAccounts()) {
            myPrincipals.add(accountToName(a));
        }
        return myPrincipals;
    }

    /**
     * Gets the process ID from a process handle.
     *
     * @param processHandle
     *            The handle of the process
     * @return The process ID
     * @throws Win32Exception
     *             if the operation fails
     * @throws UnsupportedOperationException
     *             if not running on Windows
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
     * Checks if the current user has the specified permissions for a folder.
     *
     * @param dirPath
     *            The directory path to check
     * @param permissions
     *            The permissions to verify
     * @return true if the user has all specified permissions, false otherwise
     */
    public static boolean checkFileAccessViaHandle(File dirPath, AccessPermission... permissions) {
        int access = 0;
        for (AccessPermission p : permissions) {
            access |= p.mask;
        }
        ;
        HANDLE h = Kernel32.INSTANCE.CreateFile(dirPath.getAbsolutePath(), access, WinNT.FILE_SHARE_READ | WinNT.FILE_SHARE_WRITE | WinNT.FILE_SHARE_DELETE, null, WinNT.OPEN_EXISTING, WinNT.FILE_FLAG_BACKUP_SEMANTICS, null);
        boolean hasAccess = !WinBase.INVALID_HANDLE_VALUE.equals(h);
        if (hasAccess) {
            try {
                if (Arrays.asList(permissions).contains(AccessPermission.FILE_READ_ATTRIBUTES)) {
                    ByHandleFileInformation info = new ByHandleFileInformation();
                    boolean ok = Kernel32Ext.INSTANCE.GetFileInformationByHandle(h, info);
                    if (!ok) {
                        return false;
                    }
                }
            } finally {
                Kernel32.INSTANCE.CloseHandle(h);
            }
        }
        return hasAccess;
    }

    /**
     *
     * @deprecated actually this method is NOT DEPRECATED, however you may not want to use it. To change an owner, you should run elevated
     *             and you will have to enable some privileges first (e.g. SeRestorePrivilege or SeTakeOwnershipPrivilege)
     */
    @Deprecated
    public static void setFileOwner(File path, String ownerSidString) {
        PSID psid = new WinNT.PSID(Advapi32Util.getAccountBySid(ownerSidString).sid);
        int result = Advapi32.INSTANCE.SetNamedSecurityInfo(path.getAbsolutePath(), AccCtrl.SE_OBJECT_TYPE.SE_FILE_OBJECT, WinNT.OWNER_SECURITY_INFORMATION, psid.getPointer(), null, null, null);
        if (result != WinError.ERROR_SUCCESS) {
            LogV3.warning("Change owner usualy only works elevated AND after enabling some privileges like SeRestorePrivilege or SeTakeOwnershipPrivilege");
            throw new Win32Exception(result);
        }
    }

    public static Account getFileOwnerSid(File path) {
        PointerByReference pOwnerSid = new PointerByReference();
        int result = Advapi32.INSTANCE.GetNamedSecurityInfo(path.getAbsolutePath(), AccCtrl.SE_OBJECT_TYPE.SE_FILE_OBJECT, WinNT.OWNER_SECURITY_INFORMATION, pOwnerSid, null, null, null, new PointerByReference());
        if (result != WinError.ERROR_SUCCESS) {
            throw new Win32Exception(result);
        }
        return Advapi32Util.getAccountBySid(new WinNT.PSID(pOwnerSid.getValue()));
    }

    /**
     * Checks if the current user is an administrator.
     *
     * @return true if the user is an administrator, false otherwise
     */
    public static boolean isAdministrator() {
        return isCurrentUserPartOfGroup(SID.SID_BUILTIN_ADMINISTRATORS.sid);
    }

    /**
     * Checks if the current user is part of a specific SID group. WANRING: MAY fail for local system.
     *
     * @param sid
     *            The SID to check against
     * @return true if the user is part of the specified group, false otherwise
     */
    public static boolean isCurrentUserPartOfGroup(String sid) {
        Set<Account> accounts = getCurrentUsersAccounts();
        for (Account a : accounts) {
            if (a.sidString.equals(sid)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the current process is running with elevated privileges.
     *
     * @return true if the process is elevated, false otherwise
     */
    public static boolean isElevated() {
        return Advapi32Util.isCurrentProcessElevated();
    }

    /**
     * Checks if a specific process is running with elevated privileges.
     *
     * @param pid
     *            The process ID to check
     * @return true if the process is elevated, false otherwise
     * @throws Win32Exception
     *             if the operation fails
     * @throws UnsupportedOperationException
     *             if not running on Windows
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

    /**
     * Starts a process with elevated privileges (UAC prompt will be shown).
     *
     * @param command
     *            The command to execute
     * @param workingDir
     *            The working directory (can be null)
     * @param showWindow
     *            Whether to show the window (true) or hide it (false)
     * @return The process handle if successful
     * @throws Win32Exception
     *             if the process cannot be started
     * @throws IllegalArgumentException
     *             if the command is invalid
     * @throws UnsupportedOperationException
     *             if not running on Windows
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
     * Terminates a process using its handle.
     *
     * @param processHandle
     *            The handle of the process to terminate
     * @param exitCode
     *            The exit code to set (typically 0 for normal termination)
     * @return true if the process was terminated successfully
     * @throws Win32Exception
     *             if the termination fails
     * @throws UnsupportedOperationException
     *             if not running on Windows
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
     * Gets the security descriptor for a file.
     *
     * @param absoluteFilePath
     *            The absolute path to the file
     * @return Memory object containing the security descriptor
     * @throws Win32Exception
     *             if the operation fails
     */
    private static Memory getSecurityDescriptorForFile(final String absoluteFilePath) {
        final int infoType = OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
        final IntByReference lpnSize = new IntByReference();
        boolean succeeded = Advapi32.INSTANCE.GetFileSecurity(absoluteFilePath, infoType, null, 0, lpnSize);
        if (!succeeded) {
            final int lastError = Kernel32.INSTANCE.GetLastError();
            if (W32Errors.ERROR_INSUFFICIENT_BUFFER != lastError) {
                throw new Win32Exception(lastError);
            }
        }
        final int nLength = lpnSize.getValue();
        final Memory securityDescriptorMemoryPointer = new Memory(nLength);
        succeeded = Advapi32.INSTANCE.GetFileSecurity(absoluteFilePath, infoType, securityDescriptorMemoryPointer, nLength, lpnSize);
        if (!succeeded) {
            securityDescriptorMemoryPointer.clear();
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return securityDescriptorMemoryPointer;
    }

    /**
     * Checks actual access permissions via ACL for a folder. Ths method does not only check if the user has the permissions, but also if
     * the user
     *
     * @param folder
     *            The folder to check
     * @param permissions
     *            The permissions to verify
     * @return true if the user has all specified permissions, false otherwise
     */
    public static boolean checkFileAccessViaACL(File folder, AccessPermission... permissions) {
        Memory securityDescriptorMemoryPointer = getSecurityDescriptorForFile(folder.getAbsolutePath().replace('/', '\\'));
        HANDLEByReference openedAccessToken = new HANDLEByReference();
        HANDLEByReference duplicatedToken = new HANDLEByReference();
        Win32Exception err = null;
        try {
            int desireAccess = TOKEN_IMPERSONATE | TOKEN_QUERY | TOKEN_DUPLICATE | STANDARD_RIGHTS_READ;
            HANDLE hProcess = Kernel32.INSTANCE.GetCurrentProcess();
            if (!Advapi32.INSTANCE.OpenProcessToken(hProcess, desireAccess, openedAccessToken)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            if (!Advapi32.INSTANCE.DuplicateToken(openedAccessToken.getValue(), SECURITY_IMPERSONATION_LEVEL.SecurityImpersonation, duplicatedToken)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            GENERIC_MAPPING mapping = new GENERIC_MAPPING();
            mapping.genericRead = new DWORD(FILE_GENERIC_READ);
            mapping.genericWrite = new DWORD(FILE_GENERIC_WRITE);
            mapping.genericExecute = new DWORD(FILE_GENERIC_EXECUTE);
            mapping.genericAll = new DWORD(FILE_ALL_ACCESS);
            int perm = 0;
            for (AccessPermission fp : permissions) {
                perm |= fp.mask;
            }
            DWORDByReference rights = new DWORDByReference(new DWORD(perm));
            Advapi32.INSTANCE.MapGenericMask(rights, mapping);
            PRIVILEGE_SET privileges = new PRIVILEGE_SET(1);
            privileges.PrivilegeCount = new DWORD(0);
            DWORDByReference privilegeLength = new DWORDByReference(new DWORD(privileges.size()));
            DWORDByReference grantedAccess = new DWORDByReference();
            BOOLByReference result = new BOOLByReference();
            if (!Advapi32.INSTANCE.AccessCheck(securityDescriptorMemoryPointer, duplicatedToken.getValue(), rights.getValue(), mapping, privileges, privilegeLength, grantedAccess, result)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            boolean ret = result.getValue().booleanValue();
            return ret;
        } catch (Win32Exception e) {
            err = e;
            throw err; // re-throw so finally block executed
        } finally {
            try {
                Kernel32Util.closeHandleRefs(openedAccessToken, duplicatedToken);
            } catch (Win32Exception e) {
                if (err == null) {
                    err = e;
                } else {
                    Exceptions.addSuppressed(err, e);
                }
            }
            if (securityDescriptorMemoryPointer != null) {
                securityDescriptorMemoryPointer.clear();
            }
            if (err != null) {
                throw err;
            }
        }
    }

    /**
     * Gets token information for a specific token type.
     *
     * @param <T>
     *            The type of token information structure
     * @param class1
     *            The class of the token information structure
     * @param type
     *            The type of token information to retrieve
     * @param sessionProcessToken2
     *            The process token
     * @return The token information structure
     * @throws Win32Exception
     *             if the operation fails
     */
    public static <T extends Structure> T getTokenInformation(final Class<T> class1, final int type, final HANDLE sessionProcessToken2) {
        final IntByReference tokenInformationLength = new IntByReference();
        int rc;
        if (Advapi32.INSTANCE.GetTokenInformation(sessionProcessToken2, type, (Structure) null, 0, tokenInformationLength)) {
            throw new RuntimeException("Expected GetTokenInformation to fail with ERROR_INSUFFICIENT_BUFFER");
        } else if ((rc = Kernel32.INSTANCE.GetLastError()) != WinError.ERROR_INSUFFICIENT_BUFFER) {
            throw new Win32Exception(rc);
        } else {
            try {
                final T user = class1.getDeclaredConstructor(int.class).newInstance(tokenInformationLength.getValue());
                if (!Advapi32.INSTANCE.GetTokenInformation(sessionProcessToken2, type, user, tokenInformationLength.getValue(), tokenInformationLength)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                } else {
                    return user;
                }
            } catch (final Win32Exception e) {
                throw e;
            } catch (final Throwable e) {
                throw new WTFException(e);
            }
        }
    }

    /**
     * Gets the available file permissions for a folder.
     *
     * @param folder
     *            The folder to check
     * @return Set of available AccessPermission values
     * @throws WTFException
     *             if the operation fails
     */
    public static Set<AccessPermission> getAvailableAccessPermissions(File folder) {
        try {
            WinNT.SECURITY_DESCRIPTOR absolute = new WinNT.SECURITY_DESCRIPTOR(ACL.MAX_ACL_SIZE);
            SECURITY_DESCRIPTOR_RELATIVE relative = Advapi32Util.getFileSecurityDescriptor(folder, false);
            WinNT.PSID pOwner = new WinNT.PSID(WinNT.SECURITY_MAX_SID_SIZE);
            WinNT.PSID pGroup = new WinNT.PSID(WinNT.SECURITY_MAX_SID_SIZE);
            ACL pDacl1 = new ACL(ACL.MAX_ACL_SIZE);
            ACL pSacl = new ACL(ACL.MAX_ACL_SIZE);
            IntByReference lpdwBufferLength = new IntByReference(absolute.size());
            IntByReference lpdwDaclSize = new IntByReference(ACL.MAX_ACL_SIZE);
            IntByReference lpdwSaclSize = new IntByReference(ACL.MAX_ACL_SIZE);
            IntByReference lpdwOwnerSize = new IntByReference(WinNT.SECURITY_MAX_SID_SIZE);
            IntByReference lpdwPrimaryGroupSize = new IntByReference(WinNT.SECURITY_MAX_SID_SIZE);
            if (!Advapi32.INSTANCE.MakeAbsoluteSD(relative, absolute, lpdwBufferLength, pDacl1, lpdwDaclSize, pSacl, lpdwSaclSize, pOwner, lpdwOwnerSize, pGroup, lpdwPrimaryGroupSize)) {
                throw new WTFException();
            }
            PACLByReference pDacl = new PACLByReference();
            BOOLByReference present = new BOOLByReference();
            BOOLByReference defaulted = new BOOLByReference();
            if (!Advapi32.INSTANCE.GetSecurityDescriptorDacl(absolute, present, pDacl, defaulted) || !present.getValue().booleanValue()) {
                throw new WTFException("Failed at GetSecurityDescriptorDacl ");
            }
            HANDLEByReference hToken = new HANDLEByReference();
            boolean tokenResult = Advapi32.INSTANCE.OpenProcessToken(Kernel32.INSTANCE.GetCurrentProcess(), WinNT.TOKEN_QUERY | WinNT.TOKEN_DUPLICATE, hToken);
            if (!tokenResult) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            final WinNT.TOKEN_USER tokenUser = getTokenInformation(WinNT.TOKEN_USER.class, WinNT.TOKEN_INFORMATION_CLASS.TokenUser, hToken.getValue());
            IntByReference effective = new IntByReference();
            Trustee trustee = new Trustee.ByReference();
            trustee.clear();
            trustee.pMultipleTrustee = Pointer.NULL;
            trustee.MultipleTrusteeOperation = Advapi32Ext.MULTIPLE_TRUSTEE_OPERATION.NO_MULTIPLE_TRUSTEE;
            trustee.TrusteeForm = Advapi32Ext.TRUSTEE_FORM.TRUSTEE_IS_SID;
            trustee.TrusteeType = Advapi32Ext.TRUSTEE_TYPE.TRUSTEE_IS_USER;
            trustee.ptstrName = tokenUser.User.Sid.getPointer();
            trustee.write();
            int result = Advapi32Ext.INSTANCE.GetEffectiveRightsFromAcl(pDacl.getValue(), trustee, effective);
            if (result != 0) {
                int error = Kernel32.INSTANCE.GetLastError();
                throw new Win32Exception(error);
            } else {
                int mask = effective.getValue();
                HashSet<AccessPermission> ret = new HashSet<AccessPermission>();
                for (AccessPermission fp : AccessPermission.values()) {
                    if (BinaryLogic.containsAll(mask, fp.mask)) {
                        ret.add(fp);
                    }
                }
                return ret;
            }
        } catch (final Throwable e) {
            throw new WTFException(e);
        }
    }

    /**
     * Represents a single Access Control Entry (ACE) in the Access Control List (ACL). This class encapsulates all information about
     * permissions for a specific Security Identifier (SID).
     *
     * An ACE defines the access rights that a specific user or group has to a file or directory. It contains information about: - The SID
     * (Security Identifier) of the user or group - The set of permissions granted or denied - Inheritance settings that determine how
     * permissions propagate to child objects - Whether the entry is inherited from a parent object - Whether the entry allows or denies
     * access
     *
     * @author thomas
     * @since 14.10.2018
     */
    public static class AccessPermissionEntry {
        /** The Security Identifier (SID) of the user or group */
        private final String                sid;
        /** The set of permissions associated with this entry */
        private final Set<AccessPermission> permissions;
        /** Whether this entry should be inherited by child objects */
        private boolean                     inherit;
        /** Whether this entry allows (true) or denies (false) access */
        private final boolean               allow;
        /** Whether this entry only applies to inherited objects */
        private boolean                     inheritOnly;
        /** Whether inheritance should not propagate to child objects */
        private boolean                     noPropagateInherit;
        /** Whether this entry applies to files (objects) */
        private boolean                     objectInherit;
        /** Whether this entry applies to directories (containers) */
        private boolean                     containerInherit;
        /** Whether this entry was inherited from a parent object */
        private boolean                     inherited;

        /**
         * Creates a new AccessPermissionEntry with the specified SID, access type and permissions.
         *
         * @param sid
         *            The Security Identifier (SID) of the user or group
         * @param allow
         *            Whether this entry allows (true) or denies (false) access
         * @param permissions
         *            The set of permissions to grant or deny
         */
        public AccessPermissionEntry(String sid, boolean allow, Set<AccessPermission> permissions) {
            this.sid = sid;
            this.permissions = permissions;
            this.allow = allow;
            this.inherit = false;
            this.inheritOnly = false;
            this.noPropagateInherit = false;
            this.objectInherit = true;
            this.containerInherit = true;
            this.inherited = false;
        }

        /**
         * Filters out inherited entries from the given array of AccessPermissionEntry objects. Returns only entries that are directly set
         * on the current path (not inherited).
         *
         * @param entries
         *            Array of AccessPermissionEntry objects to filter
         * @return List of AccessPermissionEntry objects that are not inherited
         */
        public static List<AccessPermissionEntry> filter(boolean includeInherited, AccessPermissionEntry... entries) {
            if (entries == null || entries.length == 0) {
                return Collections.emptyList();
            }
            List<AccessPermissionEntry> result = new ArrayList<AccessPermissionEntry>();
            for (AccessPermissionEntry entry : entries) {
                if (includeInherited == entry.isInherited()) {
                    result.add(entry);
                }
            }
            return result;
        }

        public String toString() {
            return toString(null);
        }

        public String toString(Account owner) {
            String accountName = sid;
            try {
                Account account = Advapi32Util.getAccountBySid(sid);
                accountName = accountToName(account);
            } catch (Exception e) {
                // If we can't resolve the SID, just use the SID string
            }
            // Create a concise permission summary using minimal required permissions
            Set<String> permissionGroups = new HashSet<String>();
            // Check for full control first - if present, ignore all other permissions
            if (permissions.contains(AccessPermission.FILE_ALL_ACCESS)) {
                permissionGroups.add("FULL");
            } else {
                // Only check other permissions if FULL is not set
                // Check for basic read permissions
                if (permissions.contains(AccessPermission.FILE_READ_DATA) || permissions.contains(AccessPermission.FILE_READ_ATTRIBUTES)) {
                    permissionGroups.add("READ");
                }
                // Check for basic write permissions
                if (permissions.contains(AccessPermission.FILE_WRITE_DATA) || permissions.contains(AccessPermission.FILE_APPEND_DATA) || permissions.contains(AccessPermission.FILE_ADD_FILE) || permissions.contains(AccessPermission.FILE_ADD_SUBDIRECTORY) || permissions.contains(AccessPermission.FILE_WRITE_ATTRIBUTES)) {
                    permissionGroups.add("WRITE");
                }
                // Check for basic execute permissions
                if (permissions.contains(AccessPermission.FILE_EXECUTE)) {
                    permissionGroups.add("EXECUTE");
                }
                // Check for delete permissions
                if (permissions.contains(AccessPermission.DELETE) || permissions.contains(AccessPermission.FILE_DELETE_CHILD)) {
                    permissionGroups.add("DELETE");
                }
            }
            // If no basic permissions match, list individual permissions
            if (permissionGroups.isEmpty()) {
                for (AccessPermission perm : permissions) {
                    permissionGroups.add(perm.name());
                }
            }
            boolean itseMe = isCurrentUserPartOfGroup(sid);
            if (owner != null && !itseMe) {
                itseMe = isCurrentUserPartOfGroup(owner.sidString);
            }
            // Create the overview line
            String overview = String.format("%s%s '%s' to '%s'", itseMe ? "*" : " ", allow ? "ALLOW" : "DENY ", new Joiner(",").join(permissionGroups), accountName);
            // Add detailed information
            return String.format("%s | Details: {" + "sid='%s', " + "allow=%b, " + "permissions=%s, " + "inherited=%b, " + "inherit=%b, " + "inheritOnly=%b, " + "noPropagateInherit=%b, " + "objectInherit=%b, " + "containerInherit=%b" + "}", overview, sid, allow, permissions, inherited, inherit, inheritOnly, noPropagateInherit, objectInherit, containerInherit);
        }

        /**
         * Creates a new AccessPermissionEntry that allows access.
         *
         * @param sid
         *            The Security Identifier (SID) of the user or group
         * @param permissions
         *            The set of permissions to grant
         * @return A new AccessPermissionEntry instance
         */
        public static AccessPermissionEntry allow(String sid, Set<AccessPermission> permissions) {
            return new AccessPermissionEntry(sid, true, permissions);
        }

        /**
         * Creates a new AccessPermissionEntry that denies access.
         *
         * @param sid
         *            The Security Identifier (SID) of the user or group
         * @param permissions
         *            The set of permissions to deny
         * @return A new AccessPermissionEntry instance
         */
        public static AccessPermissionEntry deny(String sid, Set<AccessPermission> permissions) {
            return new AccessPermissionEntry(sid, false, permissions);
        }

        /**
         * Sets whether this entry was inherited from a parent object.
         *
         * @param inherited
         *            true if this entry was inherited, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry inherited(boolean inherited) {
            this.inherited = inherited;
            return this;
        }

        /**
         * Sets whether this entry should be inherited by child objects.
         *
         * @param inherit
         *            true if this entry should be inherited, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry inherit(boolean inherit) {
            this.inherit = inherit;
            return this;
        }

        /**
         * Sets whether this entry only applies to inherited objects.
         *
         * @param inheritOnly
         *            true if this entry only applies to inherited objects, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry inheritOnly(boolean inheritOnly) {
            this.inheritOnly = inheritOnly;
            return this;
        }

        /**
         * Sets whether inheritance should not propagate to child objects.
         *
         * @param noPropagateInherit
         *            true if inheritance should not propagate, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry noPropagateInherit(boolean noPropagateInherit) {
            this.noPropagateInherit = noPropagateInherit;
            return this;
        }

        /**
         * Sets whether this entry applies to files (objects).
         *
         * @param objectInherit
         *            true if this entry applies to files, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry objectInherit(boolean objectInherit) {
            this.objectInherit = objectInherit;
            return this;
        }

        /**
         * Sets whether this entry applies to directories (containers).
         *
         * @param containerInherit
         *            true if this entry applies to directories, false otherwise
         * @return This instance
         */
        public AccessPermissionEntry containerInherit(boolean containerInherit) {
            this.containerInherit = containerInherit;
            return this;
        }

        /**
         * Gets whether this entry was inherited from a parent object.
         *
         * @return true if this entry was inherited, false otherwise
         */
        public boolean isInherited() {
            return inherited;
        }

        /**
         * Sets whether this entry was inherited from a parent object.
         *
         * @param inherited
         *            true if this entry was inherited, false otherwise
         */
        public void setInherited(boolean inherited) {
            this.inherited = inherited;
        }

        /**
         * Gets the Security Identifier (SID) of the user or group.
         *
         * @return The SID string
         */
        public String getSid() {
            return sid;
        }

        /**
         * Gets the set of permissions associated with this entry.
         *
         * @return The set of AccessPermission values
         */
        public Set<AccessPermission> getPermissions() {
            return permissions;
        }

        /**
         * Gets whether this entry should be inherited by child objects.
         *
         * @return true if this entry should be inherited, false otherwise
         */
        public boolean isInherit() {
            return inherit;
        }

        /**
         * Gets whether this entry allows or denies access.
         *
         * @return true if this entry allows access, false if it denies access
         */
        public boolean isAllow() {
            return allow;
        }

        /**
         * Gets whether this entry only applies to inherited objects.
         *
         * @return true if this entry only applies to inherited objects, false otherwise
         */
        public boolean isInheritOnly() {
            return inheritOnly;
        }

        /**
         * Gets whether inheritance should not propagate to child objects.
         *
         * @return true if inheritance should not propagate, false otherwise
         */
        public boolean isNoPropagateInherit() {
            return noPropagateInherit;
        }

        /**
         * Gets whether this entry applies to files (objects).
         *
         * @return true if this entry applies to files, false otherwise
         */
        public boolean isObjectInherit() {
            return objectInherit;
        }

        /**
         * Gets whether this entry applies to directories (containers).
         *
         * @return true if this entry applies to directories, false otherwise
         */
        public boolean isContainerInherit() {
            return containerInherit;
        }

        public int getInheritanceFlags() {
            int flags = 0;
            if (inheritOnly) {
                flags |= WinNT.INHERIT_ONLY_ACE;
            }
            if (noPropagateInherit) {
                flags |= WinNT.NO_PROPAGATE_INHERIT_ACE;
            }
            if (objectInherit) {
                flags |= WinNT.OBJECT_INHERIT_ACE;
            }
            if (containerInherit) {
                flags |= WinNT.CONTAINER_INHERIT_ACE;
            }
            return flags;
        }
    }

    /**
     * Gets all access control entries (ACEs) for a folder.
     *
     * @param folder
     *            The folder to get permissions for
     * @return Array of AccessPermissionEntry objects containing all ACEs
     * @throws Win32Exception
     *             if the operation fails
     * @throws UnsupportedOperationException
     *             if not running on Windows
     */
    public static AccessPermissionEntry[] getFileAccessPermissionEntries(File folder) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        // Get security descriptor
        WinNT.SECURITY_DESCRIPTOR absolute = new WinNT.SECURITY_DESCRIPTOR(ACL.MAX_ACL_SIZE);
        SECURITY_DESCRIPTOR_RELATIVE relative = Advapi32Util.getFileSecurityDescriptor(folder, false);
        WinNT.PSID pOwner = new WinNT.PSID(WinNT.SECURITY_MAX_SID_SIZE);
        WinNT.PSID pGroup = new WinNT.PSID(WinNT.SECURITY_MAX_SID_SIZE);
        ACL pDacl1 = new ACL(ACL.MAX_ACL_SIZE);
        ACL pSacl = new ACL(ACL.MAX_ACL_SIZE);
        IntByReference lpdwBufferLength = new IntByReference(absolute.size());
        IntByReference lpdwDaclSize = new IntByReference(ACL.MAX_ACL_SIZE);
        IntByReference lpdwSaclSize = new IntByReference(ACL.MAX_ACL_SIZE);
        IntByReference lpdwOwnerSize = new IntByReference(WinNT.SECURITY_MAX_SID_SIZE);
        IntByReference lpdwPrimaryGroupSize = new IntByReference(WinNT.SECURITY_MAX_SID_SIZE);
        if (!Advapi32.INSTANCE.MakeAbsoluteSD(relative, absolute, lpdwBufferLength, pDacl1, lpdwDaclSize, pSacl, lpdwSaclSize, pOwner, lpdwOwnerSize, pGroup, lpdwPrimaryGroupSize)) {
            throw new WTFException();
        }
        PACLByReference pDacl = new PACLByReference();
        BOOLByReference present = new BOOLByReference();
        BOOLByReference defaulted = new BOOLByReference();
        if (!Advapi32.INSTANCE.GetSecurityDescriptorDacl(absolute, present, pDacl, defaulted) || !present.getValue().booleanValue()) {
            throw new WTFException("Failed at GetSecurityDescriptorDacl");
        }
        ACL acl = pDacl.getValue();
        List<AccessPermissionEntry> entries = new ArrayList<AccessPermissionEntry>();
        for (int i = 0; i < acl.AceCount; i++) {
            PointerByReference acePointer = new PointerByReference();
            if (Advapi32.INSTANCE.GetAce(acl, i, acePointer)) {
                WinNT.ACCESS_ALLOWED_ACE ace = new WinNT.ACCESS_ALLOWED_ACE(acePointer.getValue());
                WinNT.PSID aceSid = ace.getSID();
                String sidString = Advapi32Util.convertSidToStringSid(aceSid);
                // Calculate inheritance flags
                WinNT.ACE_HEADER aceHeader = new WinNT.ACE_HEADER(acePointer.getValue());
                aceHeader.read();
                int aceFlags = aceHeader.AceFlags;
                boolean inherited = (aceFlags & WinNT.INHERITED_ACE) != 0;
                boolean inheritOnly = (aceFlags & WinNT.INHERIT_ONLY_ACE) != 0;
                boolean noPropagateInherit = (aceFlags & WinNT.NO_PROPAGATE_INHERIT_ACE) != 0;
                boolean objectInherit = (aceFlags & WinNT.OBJECT_INHERIT_ACE) != 0;
                boolean containerInherit = (aceFlags & WinNT.CONTAINER_INHERIT_ACE) != 0;
                boolean inherit = objectInherit || containerInherit;
                // Calculate permissions
                Set<AccessPermission> permissions = new HashSet<AccessPermission>();
                for (AccessPermission perm : AccessPermission.values()) {
                    if ((ace.Mask & perm.mask) == perm.mask) {
                        permissions.add(perm);
                    }
                }
                entries.add(aceHeader.AceType == WinNT.ACCESS_ALLOWED_ACE_TYPE ? AccessPermissionEntry.allow(sidString, permissions).inherited(inherited).inherit(inherit).inheritOnly(inheritOnly).noPropagateInherit(noPropagateInherit).objectInherit(objectInherit).containerInherit(containerInherit) : AccessPermissionEntry.deny(sidString, permissions).inherited(inherited).inherit(inherit).inheritOnly(inheritOnly).noPropagateInherit(noPropagateInherit).objectInherit(objectInherit).containerInherit(containerInherit));
            }
        }
        return entries.toArray(new AccessPermissionEntry[0]);
    }

    /**
     * @param account
     * @return
     */
    public static String accountToName(Account account) {
        String ret = account.domain;
        if (StringUtils.isNotEmpty(account.name)) {
            if (StringUtils.isNotEmpty(ret)) {
                ret += "\\";
            }
            ret += account.name;
        }
        return ret;
    }

    public static void applyPermissions(File path, boolean append, boolean blockInheritedEntries, AccessPermissionEntry... entries) {
        try {
            if (path == null) {
                throw new IllegalArgumentException("Path may not be null!");
            }
            if (entries == null || entries.length == 0) {
                throw new IllegalArgumentException("No ACL entries provided – this would remove all access (empty DACL).");
            }
            ExplicitAccess[] accessList = new ExplicitAccess[entries.length];
            for (int i = 0; i < entries.length; i++) {
                AccessPermissionEntry entry = entries[i];
                Account account = Advapi32Util.getAccountBySid(entry.sid);
                Trustee trustee = new Trustee();
                trustee.pMultipleTrustee = null;
                trustee.MultipleTrusteeOperation = Advapi32Ext.MULTIPLE_TRUSTEE_OPERATION.NO_MULTIPLE_TRUSTEE;
                trustee.TrusteeForm = Trustee.FORM_IS_SID;
                switch (account.accountType) {
                case SID_NAME_USE.SidTypeUser:
                    trustee.TrusteeType = Trustee.TYPE_IS_USER;
                    break;
                case SID_NAME_USE.SidTypeGroup:
                    trustee.TrusteeType = Trustee.TYPE_IS_GROUP;
                    break;
                case SID_NAME_USE.SidTypeDomain:
                    trustee.TrusteeType = Trustee.TYPE_IS_DOMAIN;
                    break;
                case SID_NAME_USE.SidTypeAlias:
                    trustee.TrusteeType = Trustee.TYPE_IS_ALIAS;
                    break;
                case SID_NAME_USE.SidTypeWellKnownGroup:
                    trustee.TrusteeType = Trustee.TYPE_IS_WELL_KNOWN_GROUP;
                    break;
                case SID_NAME_USE.SidTypeDeletedAccount:
                    trustee.TrusteeType = Trustee.TYPE_IS_DELETED;
                    break;
                case SID_NAME_USE.SidTypeInvalid:
                    trustee.TrusteeType = Trustee.TYPE_IS_INVALID;
                    break;
                case SID_NAME_USE.SidTypeComputer:
                    trustee.TrusteeType = Trustee.TYPE_IS_COMPUTER;
                    break;
                default:
                    trustee.TrusteeType = Trustee.TYPE_IS_UNKNOWN;
                    break;
                }
                trustee.ptstrName = new WinNT.PSID(account.sid).getPointer();
                ExplicitAccess access = new ExplicitAccess();
                access.grfAccessPermissions = 0;
                for (AccessPermission p : entry.permissions) {
                    access.grfAccessPermissions |= p.mask;
                }
                access.grfAccessMode = entry.allow ? ExplicitAccess.SET_ACCESS : ExplicitAccess.DENY_ACCESS;
                access.grfInheritance = 0;
                if (entry.inheritOnly) {
                    access.grfInheritance |= WinNT.INHERIT_ONLY_ACE;
                }
                if (entry.noPropagateInherit) {
                    access.grfInheritance |= WinNT.NO_PROPAGATE_INHERIT_ACE;
                }
                if (entry.objectInherit) {
                    access.grfInheritance |= WinNT.OBJECT_INHERIT_ACE;
                }
                if (entry.containerInherit) {
                    access.grfInheritance |= WinNT.CONTAINER_INHERIT_ACE;
                }
                access.Trustee = trustee;
                accessList[i] = access;
            }
            PointerByReference pOldDacl = new PointerByReference();
            PointerByReference pNewDacl = new PointerByReference();
            if (append) {
                // get existing dacl
                successOrException(Advapi32.INSTANCE.GetNamedSecurityInfo(path.getAbsolutePath(), AccCtrl.SE_OBJECT_TYPE.SE_FILE_OBJECT, WinNT.DACL_SECURITY_INFORMATION, null, null, pOldDacl, null, new PointerByReference()));
            }
            try {
                // merge
                successOrException(Advapi32Ext.INSTANCE.SetEntriesInAclW(accessList.length, accessList, append ? pOldDacl.getValue() : null, pNewDacl));
                int flags = WinNT.DACL_SECURITY_INFORMATION;
                if (blockInheritedEntries) {
                    flags |= WinNT.PROTECTED_DACL_SECURITY_INFORMATION;
                } else {
                    flags |= WinNT.UNPROTECTED_DACL_SECURITY_INFORMATION;
                }
                successOrException(Advapi32.INSTANCE.SetNamedSecurityInfo(path.getAbsolutePath(), AccCtrl.SE_OBJECT_TYPE.SE_FILE_OBJECT, flags, null, null, pNewDacl.getValue(), null));
            } finally {
                Pointer p = pNewDacl.getValue();
                if (p != null && !p.equals(Pointer.NULL)) {
                    Kernel32.INSTANCE.LocalFree(p);
                } else {
                    p = pOldDacl.getValue();
                    if (p != null && !p.equals(Pointer.NULL)) {
                        Kernel32.INSTANCE.LocalFree(p);
                    }
                }
            }
            System.out.println("ACL successfully applied to: " + path);
        } catch (Exception e) {
            throw new RuntimeException("Failed to set ACL", e);
        }
    }

    public static class LockInfo {
        /**
         * Type of application that has the file locked.
         */
        public enum ApplicationType {
            /**
             * Application cannot be classified as any other type
             */
            RmUnknownApp(0),
            /**
             * Application has a main window
             */
            RmMainWindow(1),
            /**
             * Application does not have a main window
             */
            RmOtherWindow(2),
            /**
             * Application is a Windows service
             */
            RmService(3),
            /**
             * Application is Windows Explorer
             */
            RmExplorer(4),
            /**
             * Application is a console application
             */
            RmConsole(5),
            /**
             * Application is critical to system operation
             */
            RmCritical(6);

            private final int value;

            ApplicationType(int value) {
                this.value = value;
            }

            public int getValue() {
                return value;
            }

            public static ApplicationType fromValue(int value) {
                for (ApplicationType type : values()) {
                    if (type.value == value) {
                        return type;
                    }
                }
                return RmUnknownApp;
            }
        }

        /**
         * Process ID of the locking process
         */
        private final int             pid;
        /**
         * Name of the application that has the file locked
         */
        private final String          appName;
        /**
         * Short name of the service if the locking process is a service
         */
        private final String          serviceName;
        /**
         * Type of application that has the file locked
         */
        private final ApplicationType applicationType;
        /**
         * Terminal Services session ID of the process
         */
        private final int             tsSessionId;

        /**
         * Creates a new LockInfo instance with information about a process that has locked a file.
         *
         * @param pid
         *            Process ID of the locking process
         * @param appName
         *            Name of the application that has the file locked
         * @param serviceName
         *            Short name of the service if the locking process is a service
         * @param applicationType
         *            Type of application
         * @param tsSessionId
         *            Terminal Services session ID of the process
         *
         */
        public LockInfo(int pid, String appName, String serviceName, int applicationType, int tsSessionId) {
            this.pid = pid;
            this.appName = appName;
            this.serviceName = serviceName;
            this.applicationType = ApplicationType.fromValue(applicationType);
            this.tsSessionId = tsSessionId;
        }

        /**
         * Gets the process ID of the locking process.
         *
         * @return Process ID
         */
        public int getPid() {
            return pid;
        }

        /**
         * Gets the name of the application that has the file locked.
         *
         * @return Application name
         */
        public String getAppName() {
            return appName;
        }

        /**
         * Gets the short name of the service if the locking process is a service.
         *
         * @return Service name or empty string if not a service
         */
        public String getServiceName() {
            return serviceName;
        }

        /**
         * Gets the type of application that has the file locked.
         *
         * @return Application type
         */
        public ApplicationType getApplicationType() {
            return applicationType;
        }

        /**
         * Gets the Terminal Services session ID of the process.
         *
         * @return Terminal Services session ID
         */
        public int getTsSessionId() {
            return tsSessionId;
        }

        @Override
        public String toString() {
            return String.format("LockInfo{pid=%d, appName='%s', serviceName='%s', type=%s, sessionId=%d}", pid, appName, serviceName, applicationType, tsSessionId);
        }
    }

    private static final int                  CCH_RM_SESSION_KEY    = 32;
    /** All possible permissions */
    public static final Set<AccessPermission> PERMISSIONSET_FULL    = EnumSet.allOf(AccessPermission.class);
    /** Read-related permissions */
    public static final Set<AccessPermission> PERMISSIONSET_READ    = EnumSet.of(AccessPermission.FILE_READ_DATA, AccessPermission.FILE_LIST_DIRECTORY, AccessPermission.FILE_READ_EA, AccessPermission.FILE_READ_ATTRIBUTES, AccessPermission.FILE_GENERIC_READ, AccessPermission.READ_CONTROL);
    /** Write-related permissions */
    public static final Set<AccessPermission> PERMISSIONSET_WRITE   = EnumSet.of(AccessPermission.FILE_WRITE_DATA, AccessPermission.FILE_APPEND_DATA, AccessPermission.FILE_ADD_FILE, AccessPermission.FILE_ADD_SUBDIRECTORY, AccessPermission.FILE_WRITE_EA, AccessPermission.FILE_WRITE_ATTRIBUTES, AccessPermission.FILE_GENERIC_WRITE, AccessPermission.WRITE_DAC, AccessPermission.WRITE_OWNER);
    /** Execute-related permissions */
    public static final Set<AccessPermission> PERMISSIONSET_EXECUTE = EnumSet.of(AccessPermission.FILE_EXECUTE, AccessPermission.FILE_TRAVERSE, AccessPermission.FILE_GENERIC_EXECUTE);
    /** Delete-related permissions */
    public static final Set<AccessPermission> PERMISSIONSET_DELETE  = EnumSet.of(AccessPermission.DELETE, AccessPermission.FILE_DELETE_CHILD);
    /** Modify = Read + Write + Execute (excluding delete) */
    public static final Set<AccessPermission> PERMISSIONSET_MODIFY;
    static {
        Set<AccessPermission> temp = EnumSet.noneOf(AccessPermission.class);
        temp.addAll(PERMISSIONSET_READ);
        temp.addAll(PERMISSIONSET_WRITE);
        temp.addAll(PERMISSIONSET_EXECUTE);
        PERMISSIONSET_MODIFY = EnumSet.copyOf(temp);
    }

    public static List<LockInfo> getLocksOnPath(File filePath) {
        IntByReference session = new IntByReference();
        char[] sessionKey = new char[CCH_RM_SESSION_KEY + 1];
        List<LockInfo> result = new ArrayList<LockInfo>();
        try {
            // Start a new Restart Manager session
            int res = Rm.INSTANCE.RmStartSession(session, 0, sessionKey);
            if (res != 0) {
                throw new Win32Exception(res);
            }
            try {
                // Register the file we're interested in
                StringArray resources = new StringArray(new WString[] { new WString(filePath.getAbsolutePath()) });
                res = Rm.INSTANCE.RmRegisterResources(session.getValue(), 1, resources, 0, Pointer.NULL, 0, null);
                if (res != 0) {
                    throw new Win32Exception(res);
                }
                // First call to get needed array size
                IntByReference needed = new IntByReference();
                IntByReference count = new IntByReference();
                res = Rm.INSTANCE.RmGetList(session.getValue(), needed, count, null, new LongByReference());
                if (res != WinNT.ERROR_MORE_DATA) {
                    throw new Win32Exception(res);
                }
                if (needed.getValue() == 0) {
                    return Collections.emptyList();
                }
                // Allocate array and call again
                RmProcessInfo[] arr = (RmProcessInfo[]) new RmProcessInfo().toArray(needed.getValue());
                count.setValue(needed.getValue());
                res = Rm.INSTANCE.RmGetList(session.getValue(), needed, count, arr, new LongByReference());
                if (res != 0) {
                    throw new Win32Exception(res);
                }
                // Process results before ending session
                for (int i = 0; i < count.getValue(); i++) {
                    try {
                        // Create a copy of the data to avoid memory issues
                        int pid = arr[i].Process.dwProcessId;
                        if (pid <= 0) {
                            continue;
                        }
                        // Safely convert strings with null checks and copying
                        String name = "";
                        String service = "";
                        try {
                            if (arr[i].strAppName != null) {
                                name = new String(arr[i].strAppName).trim();
                            }
                            if (arr[i].strServiceShortName != null) {
                                service = new String(arr[i].strServiceShortName).trim();
                            }
                        } catch (Exception e) {
                            LogV3.exception(WindowsUtils.class, e);
                        }
                        // Create LockInfo with copied data
                        result.add(new LockInfo(pid, name, service, arr[i].ApplicationType, arr[i].TSSessionId));
                    } catch (Exception e) {
                        LogV3.exception(WindowsUtils.class, e);
                        continue;
                    }
                }
            } finally {
                // End session in a separate try-catch block
                try {
                    int sessionHandle = session.getValue();
                    if (sessionHandle != 0) {
                        int endRes = Rm.INSTANCE.RmEndSession(sessionHandle);
                        if (endRes != 0) {
                            LogV3.exception(WindowsUtils.class, new Win32Exception(endRes));
                        }
                    }
                } catch (Exception e) {
                    LogV3.exception(WindowsUtils.class, e);
                }
            }
        } catch (Exception e) {
            LogV3.exception(WindowsUtils.class, e);
            if (e instanceof Win32Exception) {
                throw (Win32Exception) e;
            }
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        return result;
    }

    /**
     * @param resultCode
     */
    private static void successOrException(int resultCode) throws Win32Exception {
        if (resultCode != WinError.ERROR_SUCCESS) {
            throw new Win32Exception(resultCode);
        }
    }

    private static String escapeXml(String input) {

        if (input == null) {
            return "";
        }
        return input.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&apos;");
    }

    public static void runViaWindowsScheduler(String binary, String workingDir, String... args) throws IOException, InterruptedException {
        String taskName = "TempAppWorkJavaTask_" + UniqueAlltimeID.next();
        LogV3.info("Launch via Scheduler: " + binary + "  " + Arrays.toString(args) + " in " + workingDir);
        File file = Application.getResource("tmp/" + taskName + ".xml");
        file.delete();

        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX");
        ZonedDateTime start = ZonedDateTime.now().plusMinutes(1);
        ZonedDateTime end = start.plusMinutes(2);
        String startTime = fmt.format(start);
        String endTime = fmt.format(end);
        String argumentsXMLNode = "";
        if (args.length > 0) {
            argumentsXMLNode = "<Arguments>" + escapeXml(ShellParser.createCommandLine(Style.WINDOWS, args)) + "</Arguments>\n";
        }

     // @formatter:off
        String xml =
                "<?xml version=\"1.0\" encoding=\"UTF-16\"?>\n" +
                "<Task version=\"1.2\" xmlns=\"http://schemas.microsoft.com/windows/2004/02/mit/task\">\n" +
                "  <RegistrationInfo>\n" +
                "    <Author>JavaTask</Author>\n" +
                "  </RegistrationInfo>\n" +
                "  <Triggers>\n" +
                "    <TimeTrigger>\n" +
                "      <StartBoundary>"+escapeXml(startTime)+"</StartBoundary>\n" +
                "      <EndBoundary>"+escapeXml(endTime)+"</EndBoundary>\n" +
                "      <ExecutionTimeLimit>PT0S</ExecutionTimeLimit>\n" +
                "      <Enabled>true</Enabled>\n" +
                "    </TimeTrigger>\n" +
                "  </Triggers>\n" +
                "  <Principals>\n" +
                "    <Principal id=\"Author\">\n" +
                "      <LogonType>InteractiveToken</LogonType>\n" +
                "      <RunLevel>LeastPrivilege</RunLevel>\n" +
                "    </Principal>\n" +
                "  </Principals>\n" +
                "  <Settings>\n" +
                "    <MultipleInstancesPolicy>IgnoreNew</MultipleInstancesPolicy>\n" +
                "    <DisallowStartIfOnBatteries>false</DisallowStartIfOnBatteries>\n" +
                "    <StopIfGoingOnBatteries>false</StopIfGoingOnBatteries>\n" +
                "    <AllowHardTerminate>true</AllowHardTerminate>\n" +
                "    <StartWhenAvailable>true</StartWhenAvailable>\n" +
                "    <AllowStartOnDemand>true</AllowStartOnDemand>\n" +
                "    <Enabled>true</Enabled>\n" +
                "    <Hidden>true</Hidden>\n" +
                "    <DeleteExpiredTaskAfter>PT1M</DeleteExpiredTaskAfter>\n" +
                "  </Settings>\n" +
                "  <Actions Context=\"Author\">\n" +
                "    <Exec>\n" +
                "      <Command>"+escapeXml(binary)+"</Command>\n" +argumentsXMLNode+
           ( workingDir==null?"":    "     <WorkingDirectory>"+escapeXml(workingDir)+"</WorkingDirectory>\n") +
                "    </Exec>\n" +
                "  </Actions>\n" +
                "</Task>";
     // @formatter:on

        ByteArrayOutputStream bao = new ByteArrayOutputStream();
        bao.write(BOM.UTF16LE.getBOM());
        bao.write(xml.getBytes("UTF-16LE"));

        IO.secureWrite(file, bao.toByteArray(), SYNC.META_AND_DATA);

        try {
            try {
                LogV3.info("Create Task");
                ProcessOutput result = ProcessBuilderFactory.runCommand("schtasks", "/create", "/tn", taskName, "/xml", file.toString(), "/f");
                LogV3.info(result.toString());
                if (result.getExitCode() != 0) {
                    throw new WTFException(result.toString());
                }
                LogV3.info("Run Task");
                result = (ProcessBuilderFactory.runCommand("schtasks", "/run", "/tn", taskName));
                LogV3.info(result.toString());
                if (result.getExitCode() != 0) {
                    throw new WTFException(result.toString());
                }

                // Thread.sleep(1000);
            } finally {
                LogV3.info("Delete Task");
                ProcessOutput result = (ProcessBuilderFactory.runCommand("schtasks", "/delete", "/tn", taskName, "/f"));
                LogV3.info(result.toString());
                if (result.getExitCode() != 0) {
                    throw new WTFException(result.toString());
                }
            }
        } finally {
            file.delete();
        }
    }
}
