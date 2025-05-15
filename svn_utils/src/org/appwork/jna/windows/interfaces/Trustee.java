/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.jna.windows.interfaces;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/**
 * @author thomas
 * @date 09.05.2025
 *
 */
public class Trustee extends Structure {
    // TRUSTEE_FORM
    public static final int FORM_IS_SID              = 0;
    public static final int FORM_IS_NAME             = 1;
    public static final int FORM_BAD                 = 2;
    public static final int FORM_IS_OBJECTS_AND_SID  = 3;
    public static final int FORM_IS_OBJECTS_AND_NAME = 4;
    // TRUSTEE_TYPE
    public static final int TYPE_IS_UNKNOWN          = 0;
    public static final int TYPE_IS_USER             = 1;
    public static final int TYPE_IS_GROUP            = 2;
    public static final int TYPE_IS_DOMAIN           = 3;
    public static final int TYPE_IS_ALIAS            = 4;
    public static final int TYPE_IS_WELL_KNOWN_GROUP = 5;
    public static final int TYPE_IS_DELETED          = 6;
    public static final int TYPE_IS_INVALID          = 7;
    public static final int TYPE_IS_COMPUTER         = 8;

    /** Allows chaining to another TRUSTEE for complex ACL entries */
    public static class ByReference extends Trustee implements Structure.ByReference {
    }

    public static class ByValue extends Trustee implements Structure.ByValue {
    }

    /** pointer to another TRUSTEE (for multiple trustee operations) */
    public Pointer pMultipleTrustee;
    /** one of MULTIPLE_TRUSTEE_OPERATION constants */
    public int     MultipleTrusteeOperation;
    /** one of TRUSTEE_FORM constants */
    public int     TrusteeForm;
    /** one of TRUSTEE_TYPE constants */
    public int     TrusteeType;
    /**
     * either a PSID (when TrusteeForm == TRUSTEE_IS_SID) or an ANSI/Unicode string (when TrusteeForm == TRUSTEE_IS_NAME)
     */
    public Pointer ptstrName;

    protected List<String> getFieldOrder() {
        return Arrays.asList("pMultipleTrustee", "MultipleTrusteeOperation", "TrusteeForm", "TrusteeType", "ptstrName");
    }
}
