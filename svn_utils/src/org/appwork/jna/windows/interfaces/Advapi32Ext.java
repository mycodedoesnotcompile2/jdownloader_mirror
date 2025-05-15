package org.appwork.jna.windows.interfaces;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.W32APIOptions;

public interface Advapi32Ext extends Advapi32 {
    // Trustee form constants
    interface TRUSTEE_FORM {
        int TRUSTEE_IS_SID             = 0;
        int TRUSTEE_IS_NAME            = 1;
        int TRUSTEE_BAD_FORM           = 2;
        int TRUSTEE_IS_OBJECTS_AND_SID = 3;
    }

    // Trustee type constants
    interface TRUSTEE_TYPE {
        int TRUSTEE_IS_UNKNOWN          = 0;
        int TRUSTEE_IS_USER             = 1;
        int TRUSTEE_IS_GROUP            = 2;
        int TRUSTEE_IS_DOMAIN           = 3;
        int TRUSTEE_IS_ALIAS            = 4;
        int TRUSTEE_IS_WELL_KNOWN_GROUP = 5;
        int TRUSTEE_IS_DELETED          = 6;
        int TRUSTEE_IS_INVALID          = 7;
        int TRUSTEE_IS_COMPUTER         = 8;
    }

    // Multiple trustee operation constants
    interface MULTIPLE_TRUSTEE_OPERATION {
        int NO_MULTIPLE_TRUSTEE    = 0;
        int TRUSTEE_IS_IMPERSONATE = 1;
    }

    public final static Advapi32Ext INSTANCE = Native.load("advapi32", Advapi32Ext.class, W32APIOptions.UNICODE_OPTIONS);
    // boolean ConvertStringSidToSid(String StringSid, WinNT.PSID Sid);
    // int SetNamedSecurityInfo(String pObjectName, int ObjectType, int SecurityInfo, Pointer pointer, WinNT.PSID psidGroup, WinNT.ACL
    // pDacl, WinNT.ACL pSacl);

    /**
     * Converts a string-format security descriptor into a valid, functional security descriptor.
     *
     * @param StringSecurityDescriptor
     *            A pointer to a null-terminated string containing the string-format security descriptor to convert.
     * @param StringSDRevision
     *            The revision level of the StringSecurityDescriptor string. Currently this value must be SDDL_REVISION_1.
     * @param SecurityDescriptor
     *            A pointer to a variable that receives a pointer to the converted security descriptor.
     * @param SecurityDescriptorSize
     *            A pointer to a variable that receives the size, in bytes, of the converted security descriptor.
     * @return If the function succeeds, the return value is nonzero.
     */
    boolean ConvertStringSecurityDescriptorToSecurityDescriptor(String StringSecurityDescriptor, int StringSDRevision, PointerByReference SecurityDescriptor, Pointer SecurityDescriptorSize);

    /**
     * The GetEffectiveRightsFromAcl function retrieves the effective access rights that an ACL structure grants to a specified trustee.
     *
     * @param acl
     *            A pointer to an ACL structure from which to get the effective access rights.
     * @param pTrustee
     *            A pointer to a TRUSTEE structure that identifies the trustee.
     * @param pAccessRights
     *            A pointer to an ACCESS_MASK structure that receives the effective access rights.
     * @return If the function succeeds, it returns ERROR_SUCCESS. If the function fails, it returns a nonzero error code.
     */
    int GetEffectiveRightsFromAcl(com.sun.jna.platform.win32.WinNT.ACL acl, Trustee pTrustee, com.sun.jna.ptr.IntByReference pAccessRights);

    /**
     * The IsValidSid function validates a security identifier (SID) by verifying that the revision number is within a known range, and that
     * the number of subauthorities is less than the maximum.
     *
     * @param pSid
     *            A pointer to the SID structure to validate.
     * @return If the SID structure is valid, the return value is nonzero. If the SID structure is not valid, the return value is zero.
     */
    boolean IsValidSid(WinNT.PSID.ByReference pSid);

    /**
     * The BuildTrusteeWithSid function initializes a TRUSTEE structure with the specified SID.
     *
     * @param pTrustee
     *            A pointer to a TRUSTEE structure to initialize.
     * @param pSid
     *            A pointer to a SID structure that identifies the trustee.
     */
    void BuildTrusteeWithSidA(Trustee pTrustee, WinNT.PSID.ByReference pSid);

    int SetEntriesInAclW(int cCountOfExplicitEntries, ExplicitAccess[] pListOfExplicitEntries, Pointer OldAcl, com.sun.jna.ptr.PointerByReference NewAcl);
}