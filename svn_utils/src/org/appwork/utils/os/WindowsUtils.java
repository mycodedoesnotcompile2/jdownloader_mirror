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

import java.lang.reflect.Method;

import org.appwork.utils.StringUtils;

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

    /**
     * Returns the current user SID. WARNING: Uses Reflection and com.sun packages.
     *
     * @return
     */
    public static String getCurrentUserSID() throws Exception {
        final Class<?> ntSystemClass = Class.forName("com.sun.security.auth.module.NTSystem");
        final Object ntSystem = ntSystemClass.newInstance();
        final Method getSID = ntSystemClass.getDeclaredMethod("getUserSID", new Class[0]);
        return (String) getSID.invoke(ntSystem, new Object[0]);
    }

    /**
     * Tests if the current user is part of a SID group (e.g. {@link #SID_EVERYBODYY}) WARNING: Does not work for Local System. use
     * getCurrentUserSID() instead; WARNING: Uses Reflection and com.sun packages.
     *
     * @param sid
     * @return
     */
    public static boolean isCurrentUserPartOfGroup(String sid) throws Exception {
        final Class<?> ntSystemClass = Class.forName("com.sun.security.auth.module.NTSystem");
        final Object ntSystem = ntSystemClass.newInstance();
        final Method getGroupIDs = ntSystemClass.getDeclaredMethod("getGroupIDs", new Class[0]);
        final String groups[] = (String[]) getGroupIDs.invoke(ntSystem, new Object[0]);
        for (final String group : groups) {
            if (StringUtils.equals(sid, group)) {
                return true;
            }
        }
        return false;
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

}
